      SUBROUTINE STRC2C (B,NB,C1,YTAB, YPTAB)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-11 STRC2C Krogh   Declared all vars.
C>> 1994-10-20 STRC2C Krogh  Changes to use M77CON
C>> 1987-12-09 STRC2C Lawson  Initial code.
c--S replaces "?": ?TRC2C, ?C2BAS
c          Given C1() containing coefficients relative to a
c     cubic B-spline basis, this subr computes and stores into
c     YTAB() and YPTAB()  respectively, the value and first derivative
c     of the spline curve at each nodal point given in B().
C     INPUT..
C     (B(I),I=1,NB)     PARTITION POINTS,INCLUDING ENDPOINTS
C     NB                NO. OF POINTS IN B().
C                       NO. OF SEGMENTS IS NSEG = NB-1
c     (C1(I),I=1,NB+2)   Coefficients relative to a cubic B-spline basis
C
C     OUTPUT..
c     (YTAB(i),i=1,NB) and (YPTAB(i),i=1,NB)
c                    Values of spline function and its first
c                    derivative at the points given in B().
c                    YTAB(2*j-1) = f(B(j),   YPTAB(2*j) = fprime(B(j)).
c     ------------------------------------------------------------------
c     Based on code written by Lawson, Hanson, Lang, and Campbell, JPL,
c     1968-1974.
c     Modified July 1984 and July 1987 by C. L. Lawson, JPL, for
c     inclusion in the MATH 77 library.
c     ------------------------------------------------------------------
c     Subprogram referenced:  SC2BAS
c     ------------------------------------------------------------------
      integer NB, MB, NSEG, IB, ISEG, ISEGM1, IDERIV, I
      real             B(NB), C1(NB+2), YTAB(NB), YPTAB(NB), P(4)
      real             SUM, ZERO
      logical NEWSEG
      parameter( ZERO = 0.0E0)
c     ------------------------------------------------------------------
      MB=NB
      NSEG=MB-1
      do 30  IB=1,MB
         ISEG=min(NSEG,IB)
         ISEGM1=ISEG-1
         NEWSEG = .TRUE.
         do 20  IDERIV=0,1
            call SC2BAS(B(IB), IDERIV, ISEG, NEWSEG, B, NB, P)
            SUM=ZERO
            do 10  I=1,4
               SUM = SUM + C1(ISEGM1+I)*P(I)
   10       continue
            if(IDERIV .eq. 0) then
               YTAB(IB) = SUM
            else
               YPTAB(IB) = SUM
            endif
   20    continue
   30 continue
      return
      end
