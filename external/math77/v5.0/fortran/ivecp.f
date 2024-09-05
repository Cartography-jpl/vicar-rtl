      SUBROUTINE      IVECP  (V, N, TEXT)
c     IVECP..  Print a vector.
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-08-08 CLL Special treatment for text(1:1) .eq. '0'
c>> 1994-04-20 CLL Making code in IVECP similar to DVECP and SVECP.
C>> 1992-04-22 CLL
C>> 1985-09-20 CLL
C>> 1983-07-05 IVECP  K. Stewart   recoded for math 77 library
C>> 1981-07-23 IVECP  Kris Stewart RECODED FOR PORTABLE ENVIRONMENT
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
C     ------------------------------------------------------------------
      integer  iblock, j, j1, j2, maxcol, n, nblock
      integer           v(*)
      character*(*)     text
      parameter(maxcol = 8)
c     ------------------------------------------------------------------
      if(text(1:1) .eq. '0') then
         write(*,'(/1x,a)') text(2:)
      else
         write(*,'(a)') text
      endif
      nblock=(n+maxcol-1)/maxcol
      j2 = 0
  
      do 70 iblock = 1,nblock
         j1 = j2 + 1
         j2 = min(j1+maxcol-1, n)
         write(*,'(1x,i3,a,i3,1x,8i15)') j1,' TO ',j2,(v(j),j=j1,j2)
   70 continue
      end
