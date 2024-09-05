      real             function SRANU()
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-20 SRANU Krogh  Changes to use M77CON
c>> 1994-06-24 SRANU CLL Changed common to use RANC[D/S]1 & RANC[D/S]2.
c>> 1992-03-16 CLL
c>> 1991-11-26 CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 CLL Added call to RAN0, and SGFLAG in common.
c>> 1991-01-15 CLL Reordered common contents for efficiency.
C>> 1990-01-23 CLL  Making names in common same in all subprogams.
C>> 1987-04-22 SRANU  Lawson  Initial code.
c        Returns one pseudorandom number from the uniform distribution
c        on [0., 1.].
c     C. L. Lawson & S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
c--S replaces "?": ?RANU, ?RANUA, RANC?1, RANC?2, ?PTR, ?NUMS, ?GFLAG
C     RANCS1 and RANCS2 are common blocks.
c     Calls RAN0 to initialize SPTR and SGFLAG.
c     ------------------------------------------------------------------
      integer M
      parameter(M = 97)
      real             SNUMS(M)
      common/RANCS2/SNUMS
c
      integer SPTR
      logical SGFLAG
      common/RANCS1/SPTR, SGFLAG
      save  /RANCS1/, /RANCS2/, FIRST
      logical FIRST
      data    FIRST / .true. /
c     ------------------------------------------------------------------
      if(FIRST) then
         FIRST = .false.
         call RAN0
      endif
c
      SPTR = SPTR - 1
      if(SPTR .eq. 0) then
         call SRANUA(SNUMS, M)
         SPTR = M
      endif
      SRANU = SNUMS(SPTR)
      return
      end
