      real             function SRANR(ALPHA)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-20 SRANR Krogh  Changes to use M77CON
c>> 1994-06-24 SRANR CLL Changed common to use RANC[D/S]1 & RANC[D/S]2.
c>> 1992-03-16 CLL
c>> 1991-11-26 CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 CLL Added call to RAN0, and SGFLAG in common.
c>> 1991-01-15 CLL Reordered common contents for efficiency.
C>> 1990-01-23 CLL  Making names in common same in all subprogams.
C>> 1987-04-22 SRANR  Lawson  Initial code.
c        Returns one pseudorandom number from the Rayleigh distribution
c     with parameter, ALPHA, which should be positive.
c     If U is random, uniform on [0, 1], the Rayleigh variable is given
c     by                   SRANR = ALPHA * sqrt(-2.0 * log(U))
c     This variable has    mean = ALPHA * sqrt(Pi/2)
c     and                  variance = (2 - Pi/2) * ALPHA**2
c     Code based on subprogram written for JPL by Stephen L. Ritchie,
c     Heliodyne Corp. and Wiley R. Bunton, JPL, 1969.
c     Adapted to Fortran 77 for the JPL MATH77 library by C. L. Lawson &
c     S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
c--S replaces "?": ?RANR, ?RANUA, RANC?1, RANC?2, ?PTR, ?NUMS, ?GFLAG
C     RANCS1 and RANCS2 are common blocks.
c     Calls RAN0 to initialize SPTR and SGFLAG.
c     ------------------------------------------------------------------
      integer M
      parameter(M = 97)
      real                SNUMS(M), ALPHA, MTWO
      parameter(MTWO = -2.0E0)
      common/RANCS2/SNUMS
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
      SRANR = ALPHA * sqrt(MTWO * log(SNUMS(SPTR)))
      return
      end
