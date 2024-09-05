      SUBROUTINE ZCOEF(N,ZRT,ZC)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1995-10-27 ZCOEF  Krogh   Fixed so M77CON can get S.P. for C conv.
C>> 1994-11-11 ZCOEF  Krogh   Declared all vars.
C>> 1987-09-16 ZCOEF  Lawson  Initial code.
c--Z replaces "?": ?COEF
C
c     Given N complex numbers, this subr computes the (complex)
c     coefficients of the Nth degree monic polynomial having these
c     numbers as its roots.
c     C. L. Lawson, JPL, 1987 Feb 13.
c
c     N     [In, Integer]  Number of given roots and degree of poly.
c     ZRT() [In]  The given ith complex root is (ZRT(1,i), ZRT(2,i))
c     ZC()  [Out]  The (complex) coefficient of z**j will be stored
c           in (ZC(1,N+1-j), ZC(2,N+1-j)) for j = 0, ...,N+1.  The high
c           order coeff will be one, i.e.
c                (ZC(1,1), ZC(2,1)) = (1.0, 0.0).
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER N, I, J
      DOUBLE PRECISION ZRT(2,N),ZC(2,N+1),FAC(2)
      DOUBLE PRECISION ZCR,ZCI,RTR,RTI,TEM
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ZC(1,1) = 1.D0
      ZC(2,1) = 0.D0
      ZC(1,2) = -ZRT(1,1)
      ZC(2,2) = -ZRT(2,1)
      DO 20002 I = 2,N
        ZCR = ZC(1,I)
        ZCI = ZC(2,I)
        RTR = ZRT(1,I)
        RTI = ZRT(2,I)
        TEM = ZCR*RTR - ZCI*RTI
        ZCI = ZCR*RTI + ZCI*RTR
        ZCR = TEM
        ZC(1,I+1) = -ZCR
        ZC(2,I+1) = -ZCI
      DO 20005 J = I,2,-1
          ZCR = ZC(1,J-1)
          ZCI = ZC(2,J-1)
          FAC(1) = ZCR*RTR - ZCI*RTI
          FAC(2) = ZCR*RTI + ZCI*RTR
          ZC(1,J) = ZC(1,J) - FAC(1)
          ZC(2,J) = ZC(2,J) - FAC(2)
20005 CONTINUE
20002 CONTINUE
      RETURN
      END
