      subroutine ISTAT1(ITAB, NI, ISTATS, XSTATS, IHIST, ILOW, NCELLS)
C     .  Copyright (C) 1994, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-06-23 CLL
c     ------------------------------------------------------------------
      integer NCELLS, NI
      integer IHIST(NCELLS), ILOW, ISTATS(3), ITAB(NI)
      real    XSTATS(2)
      logical first
      save first
      data first / .true. /
c     ------------------------------------------------------------------
      if(first) then
         first = .false.
      print'(/a/a/a,a/)',
     *'  NOTICE regarding MATH77 or mathc90 library usage:',
     *'  The procedure name istat1() has been replaced by issta1().',
     *'  Please replace references to',
     *' istat1() by issta1() in your code.'
      endif
      call ISSTA1(ITAB, NI, ISTATS, XSTATS, IHIST, ILOW, NCELLS)
      end

