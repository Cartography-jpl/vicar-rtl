      subroutine ISTAT2(ISTATS, XSTATS, IHIST, ILOW, NCELLS)
C     .  Copyright (C) 1994, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-06-23 CLL
c     ------------------------------------------------------------------
      real    XSTATS(2)
      integer NCELLS
      integer IHIST(NCELLS), ILOW, ISTATS(3)
      logical first
      save first
      data first / .true. /
c     ------------------------------------------------------------------
      if(first) then
         first = .false.
      print'(/a/a/a,a/)',
     *'  NOTICE regarding MATH77 or mathc90 library usage:',
     *'  The procedure name istat2() has been replaced by issta2().',
     *'  Please replace references to',
     *' istat2() by issta2() in your code.'
      endif
         call ISSTA2(ISTATS, XSTATS, IHIST, ILOW, NCELLS)
      end

