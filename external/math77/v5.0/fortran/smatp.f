      SUBROUTINE SMATP (A,LDA,M,N,TEXT)
c     SMATP..  Print a matrix.
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-20 SMATP Krogh  Changes to use M77CON
c>> 1994-08-08 SMATP CLL Special treatment for text(1:1) .eq. '0'
c>> 1994-04-20 CLL Making DP & SP codes similar.  Calling [D/R]1MACH.
C>> 1992-04-22 CLL
C>> 1990-01-23 CLL removed extraneous "60 continue"
C>> 1985-09-20 CLL
C>> 1983-07-05 Kris Stewart For MATH77
C>> 1981-07-23 Kris Stewart Improve portability.
C>> 1969-00-00 C. L. Lawson, JPL, Original code: MOUT/VOUT
C     ------------------------------------------------------------------
C  A(,)      Matrix to be output
C  LDA       Leading dimension of array A().
C  M         No. of rows to be printed from A().
C  N         No. of cols to be printed from A().
c  TEXT   Character string to be printed as a title.
c         First character in TEXT controls line spacing before title on
c         an impact printer.  For output to be viewed on a screen it is
c         safest to always use ' '.
c         ' ' = normal single space.
c         '0' = double space.
c         '1' = page advance.
c         '+' = suppress space, i.e., overprint.
C     ------------------------------------------------------------------
c  Method:  If the machine epsilon, as given by [D/R]1MACH(3), is
c  larger than 0.5*10**(-12), we set MODE = 1 and print 8 numbers
c  accross a line, using a g15.7 format.  Otherwise we set MODE = 2 and
c  print 6 numbers across a line, using a g20.12 format.
C     ------------------------------------------------------------------
c--S replaces "?": ?MATP
C     ------------------------------------------------------------------
      integer i, iblock, j, j1, j2, lda, m, maxcol(2), mode, n, nblock
      real             a(lda,*), R1MACH
      character fmt1(2)*24, fmt2(2)*20, text*(*)
      data maxcol/8, 6/
      data fmt1 /'(/12x,8(4x,a3,i4,4x)/1x)','(/12x,6(6x,a3,i5,6x)/1x)'/
      data fmt2 /'(a,i4,4x,1p,8g15.7 )', '(a,i4,4x,1p,6g20.12)'/
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
         write(*,fmt1(mode)) ('COL',j,j=j1,j2)
         do 50 i=1,m
            write(*,fmt2(mode)) ' ROW',i,(a(i,j),j=j1,j2)
   50    continue
   70 continue
      end
