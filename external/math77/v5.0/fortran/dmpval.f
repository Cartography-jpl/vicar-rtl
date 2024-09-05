      DOUBLE PRECISION FUNCTION DMPVAL (P, NDEG, X)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-20 DMPVAL Krogh  Changes to use M77CON
C>> 1994-04-20 DMPVAL CLL Making DP and SP versions similar.
C>> 1987-12-09 DMPVAL Lawson  Initial code.
C     Evaluation of polynomial using monomial basis and a
c     linear transformation of the argument.
C     C.L.LAWSON, JPL, 1974 NOV 19
c     ------------------------------------------------------------------
c--D replaces "?": ?MPVAL
c     ------------------------------------------------------------------
      integer j, ndeg
      double precision p(*), x, u
c     ------------------------------------------------------------------
      DMPVAL=0.0d0
      u=(x-p(1))/p(2)
      do 20 j = ndeg, 0, -1
          DMPVAL=DMPVAL*u + p(j+3)
   20 continue
      end
