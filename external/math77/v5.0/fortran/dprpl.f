      subroutine DPRPL(Y,SYMBOL,IMAGE,NCHAR,Y1,Y2,RESET)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C
C>> 1994-10-20 DPRPL Krogh  Changes to use M77CON
C>> 1994-08-05 DPRPL CLL Use C = 1.5001 instead of 1.5
C>> 1992-05-12 CLL Avoid use of sign function.
C>> 1990-10-10 WV Snyder JPL Don't overflow in test for zero in interval
C>> 1988-06-24 CLL
C     ------------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     Y         Data values to be plotted.
C
C     SYMBOL    Single character to be used as plot
C               symbol.
C
C     IMAGE     Character variables in which plot image is
C               to be constructed.
C
C     NCHAR     Number of character positions in the
C               array IMAGE() to be used as a plot
C               symbol.
C
C     Y1,Y2     Numbers that bracket the range of
C               values of Y to be plotted in IMAGE().
C
C     RESET     Flag to reset the line image.If RESET
C               = .TRUE. the subroutine will:
C               1) Store NCHAR blank characters into
C                  IMAGE().
C               2) Store the character '0' in the zero
C                  value position if zero is contained
C                  in the interval [ZMIN,ZMAX].
C               3) Store the character specified by
C                  SYMBOL in the Y value position.
C
C               If RESET = .FALSE. the subroutine will
C               only execute Step 3 above.
C
C     ------------------------------------------------------
C
C     1969 July 9,programmed by C.L.Lawson,JPL
C     1981 change Hollerith characters to Fortran 77
C          characters,K.Stewart,JPL
C     1983 coded for Fortran 77,C.L.Lawson,S.Chan,JPL
C     1988-06-24 CLL Made DP version.
c     1992-05-12 CLL Noted that '0' was not being placed in IMAGE in
c       some cases when it should have on VAX and Unisys.  Was due to
c       branching on the sign of ZMIN which could be zero.  Changed
c       code to avoid use of the sign function.
C     1994-08-05 CLL Use C = 1.5001 instead of 1.5 to improve
c       consistency of symbol placement on different computers.
C     ------------------------------------------------------------------
c--D replaces "?": ?PRPL
C     ------------------------------------------------------------------
      logical RESET
      integer JY, JZ, NCHAR
      double precision A, B, C, DZ, FJZ, FNC, HALF, ONE, Y, Y1, Y2
      double precision ZERO, ZMAX, ZMIN
      character        IMAGE*(*), SYMBOL
      parameter(HALF = 0.5D0, ONE = 1.0D0, ZERO = 0.0D0)
C     ------------------------------------------------------------------
      ZMIN= min(Y1,Y2)
      ZMAX= max(Y1,Y2)
      DZ=ZMAX-ZMIN
C
C     Change ZMIN,ZMAX to avoid DZ=0
C
      if (DZ .EQ. ZERO) then
        if (ZMIN .EQ. ZERO) then
          ZMIN= -ONE
          ZMAX= +ONE
        else
          ZMIN= 0.9D0 * ZMIN
          ZMAX= 1.1D0 * ZMAX
        endif
          DZ=ZMAX-ZMIN
      endif
C
      FNC= dble(NCHAR)
C
C     Compute A,B,C to define linear transformation
C
      A=ZMIN
      B=(FNC - ONE)/DZ
      C= 1.5001D0
C
C     Set JZ = index of zero using the current A, B, C.
c     if this JZ is in the plotting range, exclusive of the end points,
c     i.e., if 1 .lt. JZ .lt. NCHAR, then recompute A,B,C to make zero
c     fall in the center of its descretization interval.  This will
c     cause data values that are symmetric around zero to plot at
c     locations that are symmetric around the zero location.
C
        JZ = -A * B + C
        if (JZ .gt. 1 .and. JZ .lt. NCHAR) then
          FJZ = dble(JZ)
          A = ZERO
          C = FJZ + HALF
          B = min( (FNC-FJZ)/ZMAX, (ONE - FJZ)/ZMIN )
        endif
C
C     End..compute A,B,C,JZ
C
      if (RESET) then
        if (NCHAR .ne. 0) IMAGE(1:NCHAR)= ' '
        if (JZ .ge. 1 .and. JZ .le. NCHAR) IMAGE(JZ:JZ)= '0'
      endif
C
C     Process Y
C
      JY=(Y-A) * B + C
      if (JY .gt. 0) then
        if (JY .le. NCHAR) then
          IMAGE(JY:JY)=SYMBOL
        else if (NCHAR .ge. 3) then
          IMAGE(NCHAR-2:NCHAR)='OUT'
        endif
      else if (NCHAR .ge. 3) then
        IMAGE(1:3)='OUT'
      endif
      end
