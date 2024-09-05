      integer function IRANP(XMEAN)
C     .  Copyright (C) 1994, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-06-23 CLL 
c     ------------------------------------------------------------------
      integer isranp
      real XMEAN
      logical first
      save first
      data first / .true. /
c     ------------------------------------------------------------------
      if(first) then
         first = .false.
      print'(/a/a/a/)',
     *'  NOTICE regarding MATH77 or mathc90 library usage:',
     *'  The function name iranp() has been replaced by isranp().',
     *'  Please replace references to iranp() by isranp() in your code.'
      endif
      iranp = isranp(xmean)
      end                
