      SUBROUTINE      SVECP  (V, N, TEXT)
c     SVECP..  Print a vector.
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-20 SVECP Krogh  Changes to use M77CON
c>> 1994-08-08 SVECP CLL Special treatment for text(1:1) .eq. '0'
c>> 1994-04-20 CLL Making DP & SP codes similar.  Calling [D/R]1MACH.
C>> 1992-04-22 CLL
C>> 1985-09-20 CLL
C>> 1983-07-05 SVECP  K. Stewart   recoded for math 77 library
C>> 1981-07-23 SVECP  Kris Stewart RECODED FOR PORTABLE ENVIRONMENT
C>> 1969-00-00 C. L. Lawson, JPL, Original code: MOUT/VOUT
c     ------------------------------------------------------------------
c                  Subroutine arguments
C
C  V(I),I=1,N    Vector to be output.
C  N             Number of vector components.
c  TEXT   Character string to be printed as a title.
c         First character in TEXT controls line spacing before title on
c         an impact printer.  For output to be viewed on a screen it is
c         safest to always use ' '.
c         ' ' = normal single space.
c         '0' = double space.
c         '1' = page advance.
c         '+' = suppress space, i.e., overprint.
c     ------------------------------------------------------------------
c  Method:  If the machine epsilon, as given by [D/R]1MACH(3), is
c  larger than 0.5*10**(-12), we set MODE = 1 and print 8 numbers
c  accross a line, using a g15.7 format.  Otherwise we set MODE = 2 and
c  print 6 numbers across a line, using a g20.12 format.
C     ------------------------------------------------------------------
c--S replaces "?": ?VECP
c     ------------------------------------------------------------------
      integer  iblock, j, j1, j2, maxcol(2), mode, n, nblock
      real             R1MACH
      real             v(*)
      character     fmt(2)*26, text*(*)
      data maxcol/8, 6/
      data fmt /'(1x,i3,a,i3,1x,1p,8g15.7 )',
     *          '(1x,i3,a,i3,1x,1p,6g20.12)'/
c     ------------------------------------------------------------------
      if(text(1:1) .eq. '0') then
         write(*,'(/1x,a)') text(2:)
      else
         write(*,'(a)') text
      endif
      if(R1MACH(3) .gt. 0.5e-12) then
         mode = 1
      else
         mode = 2
      endif

      nblock=(n+maxcol(mode)-1)/maxcol(mode)
      j2 = 0

      do 70 iblock = 1,nblock
         j1 = j2 + 1
         j2 = min(j1+maxcol(mode)-1, n)
         write(*,fmt(mode)) j1,' TO ',j2,(v(j),j=j1,j2)
   70 continue
      end
