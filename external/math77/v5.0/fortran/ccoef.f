      SUBROUTINE CCOEF(N, CROOT, CCOF)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-11 CCOEF  Krogh   Declared all vars.
C>> 1987-02-25 CCOEF  Lawson  Initial code.
C
c     Given N complex numbers, this subr computes the (complex)
c     coefficients of the Nth degree monic polynomial having these
c     numbers as its roots.
c     C. L. Lawson, JPL, 1987 Feb 13.
c
c     N     [In, Integer]  Number of given roots and degree of poly.
c     CROOT() [In, Complex]  The given ith complex root is CROOT(i).
c     CCOF()  [Out, Complex]  The (complex) coefficient of z**j will be
c           stored in CCOF(N+1-j) for j = 0, ...,N+1.  The high
c           order coeff will be one, i.e. CCOF(1) = (1.0, 0.0).
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER N, I, J
      COMPLEX CROOT(N), CCOF(N+1), CROOTI
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CCOF(1) = 1.E0
      CCOF(2) = -CROOT(1)
      DO 20002 I = 2,N
        CROOTI = CROOT(I)
        CCOF(I+1) = -CCOF(I) * CROOTI
 
C        CCOFR = CCOF(1,I)
C        CCOFI = CCOF(2,I)
C        RTR = CROOT(1,I)
C        RTI = CROOT(2,I)
C        TEM = CCOFR*RTR - CCOFI*RTI
C        CCOFI = CCOFR*RTI + CCOFI*RTR
C        CCOFR = TEM
C        CCOF(1,I+1) = -CCOFR
C        CCOF(2,I+1) = -CCOFI
 
      DO 20005 J = I,2,-1
           CCOF(J) = CCOF(J) - CCOF(J-1) * CROOTI
 
c         CCOFR = CCOF(1,J-1)
c         CCOFI = CCOF(2,J-1)
c         FAC(1) = CCOFR*RTR - CCOFI*RTI
c         FAC(2) = CCOFR*RTI + CCOFI*RTR
c         CCOF(1,J) = CCOF(1,J) - FAC(1)
c         CCOF(2,J) = CCOF(2,J) - FAC(2)
20005 CONTINUE
20002 CONTINUE
      RETURN
      END
