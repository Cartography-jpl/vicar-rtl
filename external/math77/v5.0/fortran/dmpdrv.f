      SUBROUTINE DMPDRV (C,NC,D,ND)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-20 DMPDRV Krogh  Changes to use M77CON
C>> 1987-12-09 DMPDRV Lawson  Initial code.
C     DERIVATIVE OF POLYNOMIAL REPRESENTED BY COEFFS REL TO THE
C     MONOMIAL BASIS INCLUDING A LINEAR TRANSFORMATION OF THE
C     ARGUMENT.
C     C.L.LAWSON, JPL, 1973 DEC 6
C
C     (C(I),I=1,2)       SCALE FACTORS
C     (C(I+2),I=1,NA+1)  COEFS OF POLY
C     NC                 DEGREE OF POLY
C     (D(I),I=1,2)       OUTPUT..  SCALE FACTORS
C     (D(I+2),I=1,ND+1)OUTPUT..COEFSOFDIFFERENTIATED
C                                  POLY
C     ND                 OUTPUT..  DEGREE OF POLY
c     -----------------------------------------------------------------
c--D replaces "?": ?MPDRV
c     Both versions use IERM1
c     -----------------------------------------------------------------
      integer i, nc, ncp2, nd
      double precision d(*), c(*), fac
c     -----------------------------------------------------------------
      if (nc .lt. 0) then
        call IERM1('DMPDRV',1,0,'REQUIRE NC .GE. 0','NC',nc,'.')
      else
        d(1)=c(1)
        d(2)=c(2)
        if (nc .eq. 0) then
          nd=0
          d(3)=0.0d0
        else
          ncp2=nc+2
          nd  =ncp2-3
          fac =0.0d0
          do 20 i=3,ncp2
            fac=fac+1.0d0
   20       d(i)=c(i+1)*fac/d(2)
        endif
      end if
      return
      end
